package history

type (
	History interface {
		GetContent() []*Query
	}
	Query struct {
		Id       string
		request  *Content
		response *Content
	}
	Content interface {
		GetId() string
		GetContent() string
	}
	TextContent struct {
		Id      string
		content string
	}
	ImageContent struct {
		Id      string
		content string
		url     string
	}
)

func (t TextContent) GetId() string {
	return t.Id
}

func (t TextContent) GetContent() string {
	return t.content
}

func (i ImageContent) GetId() string {
	return i.Id
}

func (i ImageContent) GetContent() string {
	return i.content
}
